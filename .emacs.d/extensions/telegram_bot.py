#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Telegram ↔ Emacs gptel-agent Bridge (File-based Polling Version)

Overview
--------
This script implements a Telegram bot that bridges messages to an Emacs gptel-agent and returns responses back to Telegram.

Instead of using stdout streaming or HTTP callbacks, it relies on a file-based polling mechanism:

Telegram → Python → Emacs (gptel-agent)
                         ↓
                 append to file
                         ↓
               Python polls file
                         ↓
             Send full response to Telegram


Key Features
------------
- Works reliably with emacsclient in daemon mode
- Supports proxy for restricted networks
- Supports file download
- Cleans up old gptel-agent buffers before starting

Configuration
-------------
- TOKEN: Telegram bot token (from BotFather)
- AUTHORIZED_USER_ID: Only this user can interact with the bot
- PROXY_URL: SOCKS5/HTTP proxy (e.g., http://127.0.0.1:1080)
- AGENT_OUTPUT_FILE: File used for streaming agent output
- TELEGRAM_MAX_LENGTH: Safe limit for Telegram messages (~4000)

Dependencies
------------
- python-telegram-bot >= v20
- Emacs with gptel and gptel-agent configured
- emacsclient (Emacs daemon must be running)
- Optional: python-telegram-bot[socks] for proxy support

    pip install python-telegram-bot --upgrade

Design Decisions
----------------
Why file-based instead of stdout streaming?

- emacsclient output buffering is unreliable
- force-output is not portable across Emacs versions
- subprocess pipes can block or deadlock
- file polling is simple, observable, and robust

Trade-offs:
- Slight latency (poll interval)
- Requires filesystem access
- Not true token-level streaming (chunk-based)

Limitations
-----------
- Single session only (shared file)
- No real-time streaming
- Entire response sent at once
- Telegram message size limit (~4000 chars per chunk)

Possible Improvements
---------------------
- Per-user or per-session output files (multi-user support)
- Smarter diffing (only send delta instead of full text)
- Overflow handling (split into multiple Telegram messages)
- Add /stop command to terminate agent
- Switch to file watcher (inotify) instead of polling

Security Notes
--------------
- Never commit your TOKEN to version control
- Regenerate token via BotFather if exposed
- Restrict access using AUTHORIZED_USER_ID

Author Intent
-------------
This implementation prioritizes reliability and simplicity over
low-latency streaming, making it suitable for constrained environments
(e.g., behind proxies or unstable IPC setups).
"""

import subprocess
import os
from telegram import Update
from telegram.ext import (
    ApplicationBuilder,
    ContextTypes,
    CommandHandler,
    MessageHandler,
    filters,
)
import logging
import asyncio
import shutil


# ================= CONFIG =================
# Replace with your actual token from BotFather
TOKEN = 'xxxxx'
# Replace with your actual Telegram User ID
AUTHORIZED_USER_ID = 12345678

PROXY_URL = "http://127.0.0.1:1080"

TELEGRAM_MAX_LENGTH = 4000
AGENT_OUTPUT_FILE = os.path.expanduser("/tmp/agent/agent-session.md")
AGENT_MEDIA_DIR = os.path.expanduser("/tmp/agent/media-file/")
SCHEDULE_FILE = os.path.expanduser("/tmp/agent/schedule.json")
LOCK_FILE = os.path.expanduser("/tmp/agent/.lock")
# =========================================

# ================= COMMAND =================
CLEAR_SESSION_COMMAND = "clear"
PROLONG_SESSION_COMMAND = "prolong"
# =========================================

# ================= GLOBAL =================
SESSION_PROLONGED = False
# =========================================

os.makedirs(AGENT_MEDIA_DIR, exist_ok=True)

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)


# ---------- Clear agent ----------
def clear_agent_session():
    elisp = f"""
(when-let ((buf (seq-find
                 (lambda (b) (string-match-p "^\\*gptel-telegram:" (buffer-name b)))
                 (buffer-list))))
  (with-current-buffer buf
    (erase-buffer)))
"""

    subprocess.Popen(
        ["emacsclient", "--eval", elisp],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )


# ---------- Emacs agent ----------
def setup_agent():
    elisp = f"""
(progn
  (defun extract_agent_response (beg end)
    (let ((pos beg)
          (result "")
          (last-prop nil))
      (while (< pos end)
        (let* ((next (next-single-property-change pos 'gptel nil end))
               (next (or next end))
               (prop (get-text-property pos 'gptel))
               (chunk (buffer-substring-no-properties pos next)))
          (when (and last-prop
                     (not (eq prop last-prop))
                     (> (length result) 0)
                     (not (string-suffix-p "\n" result))
                     (not (string-prefix-p "\n" chunk)))
            (setq result (concat result "\n")))
          (when (eq prop 'response)
            (setq result (concat result chunk)))
          (setq last-prop prop)
          (if (= pos next)
              (setq pos (1+ pos))
            (setq pos next))))
      result))

  (defun append-agent-output-to-file (beg end)
    "Append the latest gptel response to a file."
    (let ((txt (extract_agent_response beg end)))
      (with-temp-buffer
        (insert (format "Date: %s\\n" (current-time-string)))
        (insert "---\\n")
        (insert txt)
        (append-to-file (point-min) (point-max) "{AGENT_OUTPUT_FILE}"))))

  (defun get-agent-buffer ()
    (let ((buf (seq-find
                (lambda (b) (string-match-p "^\\*gptel-telegram:" (buffer-name b)))
                (buffer-list))))
      buf)))
"""

    subprocess.Popen(
        ["emacsclient", "--eval", elisp],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )


def start_agent(prompt: str):
    prompt = prompt.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")

    elisp = f"""
(progn
  (remove-hook 'gptel-post-response-functions #'append-agent-output-to-file)
  (add-hook 'gptel-post-response-functions #'append-agent-output-to-file)

  (unless (get-agent-buffer) (gptel-telegram "./"))

  (when-let ((buf (get-agent-buffer)))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert "{prompt}")
      (gptel-send))))
"""

    subprocess.Popen(
        ["emacsclient", "--eval", elisp],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )


# ---------- Send text ----------
async def send_text(text: str, update: Update, app = None):
    if not text.strip():
        return

    chunks = [
        text[i:i + TELEGRAM_MAX_LENGTH]
        for i in range(0, len(text), TELEGRAM_MAX_LENGTH)
    ]

    for chunk in chunks:
        if update:
            await update.message.reply_text(chunk)
        elif app:
            await app.bot.send_message(
                chat_id=AUTHORIZED_USER_ID,
                text=chunk)
        asyncio.sleep(0.3)  # avoid flooding telegram


# ---------- Send file ----------
async def send_files(update: Update, app):
    files = sorted(
        [os.path.join(AGENT_MEDIA_DIR, f) for f in os.listdir(AGENT_MEDIA_DIR)],
        key=os.path.getmtime
    )

    for path in files:
        if not os.path.isfile(path):
            continue

        try:
            with open(path, "rb") as f:
                if update:
                    await update.message.reply_document(document=f)
                elif app:
                    await app.bot.send_document(
                        chat_id=AUTHORIZED_USER_ID,
                        document=f)
        except Exception as e:
            await send_text(f"❌ Failed to send file: {path}", update, app)


def cleanup():
    if os.path.exists(AGENT_OUTPUT_FILE):
        os.remove(AGENT_OUTPUT_FILE)

    for item in os.listdir(AGENT_MEDIA_DIR):
        item_path = os.path.join(AGENT_MEDIA_DIR, item)
        if os.path.isdir(item_path):
            shutil.rmtree(item_path)
        else:
            os.remove(item_path)


# ----------Python polling ----------
async def poll_agent_output(update: Update, app: ApplicationBuilder):
    global SESSION_PROLONGED

    # wait 5 minutes for long session, 2 minutes for normal session
    max_polls = 300 if SESSION_PROLONGED else 120
    poll_count = 0
    SESSION_PROLONGED = False

    send_text("🧠 Thinking...", update, app)

    while poll_count < max_polls:
        if os.path.exists(AGENT_OUTPUT_FILE) and (os.path.getsize(AGENT_OUTPUT_FILE) > 0):
            with open(AGENT_OUTPUT_FILE, "r") as f:
                text = f.read()
                if text.strip():
                    await send_text(text, update, app)

            if len(os.listdir(AGENT_MEDIA_DIR)) > 0:
                await send_files(update, app)

            break

        asyncio.sleep(1)
        poll_count += 1

    cleanup()

    await send_text("✅ Agent finished.", update, app)


# ----------Task scheduling ----------
def load_schedule():
    if not os.path.exists(SCHEDULE_FILE):
        return []

    with open(SCHEDULE_FILE, "r") as f:
        return json.load(f)


def update_schedule(tasks):
    with open(SCHEDULE_FILE, "w") as f:
        json.dump(tasks, f, indent=2)


def is_due(task):
    if task.get("done"):
        return False

    run_at = datetime.strptime(task["run_at"], "%Y-%m-%d %H:%M")
    return datetime.now() >= run_at


async def run_task(task, app):
    if os.path.exists(LOCK_FILE):
        send_text("⚠️ Another task running, skip", None, app)
        return

    # create lock
    open(LOCK_FILE, "w").close()

    try:
        cleanup()
        prompt = task["prompt"]
        send_text(f"🚀 Running task: {prompt}", None, app)
        start_agent(prompt)
        await poll_agent_output(None, app)
        task["done"] = True
    finally:
        # release lock
        if os.path.exists(LOCK_FILE):
            os.remove(LOCK_FILE)


async def check_and_run_tasks(app):
    tasks = load_schedule()
    updated = []

    for task in tasks:
        if is_due(task):
            await run_task(task, app)
        updated.append(task)

    update_schedule(updated)


async def scheduler_loop(app):
    while True:
        try:
            await check_and_run_tasks(app)
        except Exception as e:
            send_text(f"Scheduler error: {e}", None, app)

        await asyncio.sleep(60)


# ---------- Telegram handlers ----------
async def start(update: Update):
    await send_text("🚀 Emacs Agent Ready (Proxy Enabled)", update)


async def handle_message(update: Update, context: ContextTypes.DEFAULT_TYPE):
    if update.message.from_user.id != AUTHORIZED_USER_ID:
        await send_text("Unauthorized", update)
        return

    prompt = update.message.text

    cleanup()

    if prompt.lower().strip() == CLEAR_SESSION_COMMAND:
        clear_agent_session()
        await send_text("✅ Agent cleared.", update)
        return

    if prompt.lower().strip() == PROLONG_SESSION_COMMAND:
        global SESSION_PROLONGED;
        SESSION_PROLONGED = True
        await send_text("✅ Agent prolonged.", update)
        return

    start_agent(prompt)
    await poll_agent_output(update)


# ---------- Main ----------
def main():
    app = (
        ApplicationBuilder()
        .token(TOKEN)
        .proxy(PROXY_URL)
        .build()
    )

    app.add_handler(CommandHandler("start", start))
    app.add_handler(MessageHandler(filters.TEXT & (~filters.COMMAND), handle_message))
    setup_agent()

    async def post_init(app):
        asyncio.create_task(scheduler_loop(app))

    app.post_init = post_init
    app.run_polling()


if __name__ == "__main__":
    main()
