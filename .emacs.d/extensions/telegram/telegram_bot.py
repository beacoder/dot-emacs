#!/usr/bin/env python3
# -*- coding: utf-8 -*-

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
import json
from datetime import datetime, timedelta
from calendar import monthrange
import sys


# ================= CONFIG =================
# Replace with your actual token from BotFather
TOKEN = 'xxxxx'
# Replace with your actual Telegram User ID
AUTHORIZED_USER_ID = 12345678

PROXY_URL = "http://127.0.0.1:1080"

TELEGRAM_MAX_LENGTH = 4000
AGENT_OUTPUT_FILE = os.path.expanduser("/home/huming/agent/agent-session.md")
AGENT_MEDIA_DIR = os.path.expanduser("/home/huming/agent/media-file/")
AGENT_SCHEDULE_FILE = os.path.expanduser("/home/huming/agent/schedule.json")
AGENT_LOCK_FILE = os.path.expanduser("/home/huming/agent/.lock")
# =========================================

# ================= COMMAND =================
CLEAR_SESSION_COMMAND = "clear"
RESTART_AGENT_COMMAND = "restart"
# =========================================

os.makedirs(AGENT_MEDIA_DIR, exist_ok=True)

logging.basicConfig(
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    level=logging.INFO
)

async def restart_agent():
    subprocess.run(
        ["pkill", "-f", "emacs"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )
    await asyncio.sleep(2)

    subprocess.Popen(
        ["emacs"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )
    await asyncio.sleep(30)

    def _restart_process():
        os.execv(sys.executable, [sys.executable] + sys.argv)

    # schedule restart AFTER current task finishes
    loop = asyncio.get_running_loop()
    loop.call_soon(_restart_process)

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

def is_agent_running():
    try:
        result = subprocess.run(
            ["emacsclient", "-e", "t"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=2)
        return result.returncode == 0
    except Exception:
        return False

def setup_agent():
    elisp = f"""
(progn
  (defun agent_extract_response (beg end)
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

  (defun agent-append-output-to-file (beg end)
    "Append the latest gptel response to a file."
    (let ((txt (agent_extract_response beg end)))
      (with-temp-buffer
        (insert (format "Date: %s\\n" (current-time-string)))
        (insert "---\\n")
        (insert txt)
        (append-to-file (point-min) (point-max) "{AGENT_OUTPUT_FILE}"))))

  (defun agent-get-buffer ()
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

async def start_agent(prompt: str):
    if not is_agent_running():
        await restart_agent()
        return

    prompt = prompt.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")

    elisp = f"""
(progn
  (remove-hook 'gptel-post-response-functions #'agent-append-output-to-file)
  (add-hook 'gptel-post-response-functions #'agent-append-output-to-file)

  (unless (agent-get-buffer) (gptel-telegram "./"))

  (when-let ((buf (agent-get-buffer)))
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
        await asyncio.sleep(0.6)  # avoid flooding telegram

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

async def poll_agent_output(max_polls: int, update: Update, app: ApplicationBuilder = None):
    poll_count = 0

    while poll_count < max_polls:
        if os.path.exists(AGENT_OUTPUT_FILE) and (os.path.getsize(AGENT_OUTPUT_FILE) > 0):
            with open(AGENT_OUTPUT_FILE, "r") as f:
                text = f.read()
                if text.strip():
                    await send_text(text, update, app)

            if len(os.listdir(AGENT_MEDIA_DIR)) > 0:
                await send_files(update, app)

            break

        await asyncio.sleep(1)
        poll_count += 1

    cleanup()

def load_schedule():
    if not os.path.exists(AGENT_SCHEDULE_FILE):
        return []

    try:
        with open(AGENT_SCHEDULE_FILE, "r") as f:
            content = f.read().strip()
            if content:
                return json.loads(content)
            return []
    except Exception as e:
        print(f"[schedule] load failed: {e}")
        return []

def update_schedule(tasks):
    tmp_file = AGENT_SCHEDULE_FILE + ".tmp"

    with open(tmp_file, "w") as f:
        json.dump(tasks, f, indent=2)

    os.replace(tmp_file, AGENT_SCHEDULE_FILE)

def is_due(task):
    if task.get("done"):
        return False

    run_at = datetime.strptime(task["run_at"], "%Y-%m-%d %H:%M")
    return datetime.now() >= run_at

def compute_next_run(task):
    repeat = task.get("repeat")
    if not repeat:
        return None

    current_run = datetime.strptime(task["run_at"], "%Y-%m-%d %H:%M")

    # Daily
    if repeat == "daily":
        next_run = current_run + timedelta(days=1)

    # Weekly: "weekly:1" (Monday=1)
    elif repeat.startswith("weekly:"):
        weekday = int(repeat.split(":")[1])
        next_run = current_run + timedelta(days=1)
        while next_run.isoweekday() != weekday:
            next_run += timedelta(days=1)

    # Monthly: "monthly:15"
    elif repeat.startswith("monthly:"):
        day = int(repeat.split(":")[1])
        year, month = current_run.year, current_run.month
        month += 1
        if month > 12:
            month = 1
            year += 1
        max_day = monthrange(year, month)[1]
        next_day = min(day, max_day)
        next_run = current_run.replace(year=year, month=month, day=next_day)

    # Interval: "interval:10m" or "interval:2h"
    elif repeat.startswith("interval:"):
        val = repeat.split(":")[1]
        if val.endswith("m"):
            minutes = int(val[:-1])
            next_run = current_run + timedelta(minutes=minutes)
        elif val.endswith("h"):
            hours = int(val[:-1])
            next_run = current_run + timedelta(hours=hours)
        else:
            next_run = None
    else:
        next_run = None

    if next_run:
        return next_run.strftime("%Y-%m-%d %H:%M")
    return None

async def run_task(task, app):
    if os.path.exists(AGENT_LOCK_FILE):
        await send_text("⚠️ Another task running, skip", None, app)
        return

    # create lock
    open(AGENT_LOCK_FILE, "w").close()

    prompt = task["prompt"]
    await send_text(f"🚀 Running task: {prompt}", None, app)

    try:
        cleanup()
        await send_text("🧠 Thinking...", None, app)
        await start_agent(prompt)
        await poll_agent_output(120, None, app)
        await send_text("✅ Agent finished.", None, app)
    finally:
        # release lock
        if os.path.exists(AGENT_LOCK_FILE):
            os.remove(AGENT_LOCK_FILE)

async def check_and_run_tasks(app):
    await poll_agent_output(2, None, app)

    tasks = load_schedule()
    updated = []

    for task in tasks:
        if is_due(task):
            await run_task(task, app)

            next_run = compute_next_run(task)
            if next_run:
                task["run_at"] = next_run
                task["done"] = False
            else:
                task["done"] = True

        updated.append(task)

    if len(updated) > 0:
        update_schedule(updated)

async def scheduler_loop(app):
    while True:
        try:
            await check_and_run_tasks(app)
        except Exception as e:
            await send_text(f"❌ Scheduler error: {e}", None, app)

        await asyncio.sleep(30)

# ---------- Telegram handlers ----------
async def handle_message(update: Update, context: ContextTypes.DEFAULT_TYPE):
    if update.message.from_user.id != AUTHORIZED_USER_ID:
        await send_text("❌ Unauthorized.", update)
        return

    if os.path.exists(AGENT_LOCK_FILE):
        await send_text("⚠️ Another task running, but we will continue", update)

    prompt = update.message.text

    if prompt.lower().strip() == CLEAR_SESSION_COMMAND:
        clear_agent_session()
        await send_text("✅ Agent session cleared.", update)
        return

    if prompt.lower().strip() == RESTART_AGENT_COMMAND:
        await send_text("⚠️ Agent restarting...", update)
        await restart_agent()
        return

    # create lock
    open(AGENT_LOCK_FILE, "w").close()

    try:
        cleanup()
        await send_text("🧠 Thinking...", update)
        await start_agent(prompt)
        await poll_agent_output(120, update)
        await send_text("✅ Agent finished.", update)
    finally:
        # release lock
        if os.path.exists(AGENT_LOCK_FILE):
            os.remove(AGENT_LOCK_FILE)

# ---------- Main ----------
def main():
    app = (
        ApplicationBuilder()
        .token(TOKEN)
        .proxy(PROXY_URL)
        .build()
    )

    app.add_handler(MessageHandler(filters.TEXT & (~filters.COMMAND), handle_message))
    setup_agent()

    async def error_handler(update, context):
        logging.error(f"Exception: {context.error}")
    app.add_error_handler(error_handler)

    async def _post_init(app):
        asyncio.create_task(scheduler_loop(app))
        await send_text("🚀 Agent ready.", None, app)
    app.post_init = _post_init

    app.run_polling()


if __name__ == "__main__":
    main()
