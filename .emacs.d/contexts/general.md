# General Rules

## Directory Configuration
- **Default Search Path**: `/tmp/agent/` (used when no specific directory is provided)
- **Default Path for storing pictures/videos**: `/tmp/agent/media-file/`
- **Forbidden Path**: `/mnt/` (no read/write/modify is allowed for this path)

## Coding Standards
- **C++**: Use C++20 conventions and features where applicable

## Commands
- Chrome:
/usr/bin/google-chrome --remote-debugging-port=9222 --user-data-dir=/tmp/chrome-profile-stable

- Telegram Bot:
python3 ~/.emacs.d/extensions/telegram_bot.py > /dev/null 2>&1

# Task Scheduling Rules

## Trigger Conditions
When user asks about:
- reminders
- scheduling
- run later
- 定时执行 / 稍后执行 / 定时任务 / 提醒

You MUST create a scheduled task.

---

## Step 1: Extract Task
You MUST extract:

- prompt: a clear, concise task description (NO time info inside)
- run_at: absolute time in format "YYYY-MM-DD HH:MM"
- done: false

---

## Step 2: Time Rules (STRICT)

- ALWAYS convert relative time to absolute time
- Use system current time as reference
- Use 24-hour format
- Format MUST be exactly:

YYYY-MM-DD HH:MM

Examples:
- 明天早上9点 → 2026-03-25 09:00
- 10分钟后 → (current time + 10 minutes)

---

## Step 3: File Operation (STRICT)

Target file:
/tmp/agent/schedule.json

### MUST follow EXACT steps:

1. If file exists:
   - Read file
   - If empty → treat as []
   - Parse JSON array

2. If file does NOT exist:
   - Initialize as empty array []

3. Append new task object

4. Write FULL JSON array back to file

---

## Step 4: JSON Format (STRICT)

Final file MUST be a VALID JSON ARRAY:

[
  {
    "prompt": "...",
    "run_at": "YYYY-MM-DD HH:MM",
    "done": false
  }
]

---

## Step 5: Hard Constraints (CRITICAL)

- NEVER overwrite existing tasks
- NEVER write partial JSON
- NEVER write invalid JSON
- NEVER include comments
- NEVER include trailing commas
- NEVER include markdown
- NEVER include explanation text in file

---

## Step 6: Tool Usage

You MUST use:

- Read → to read existing file
- Write → to write updated JSON

---

## Step 7: Behavior Rules

- DO NOT execute the task immediately
- DO NOT call external tools for execution
- ONLY schedule

---

## Example

User:
明天早上9点提醒我看比特币价格

Steps:

1. Extract:
   prompt = "查看比特币价格"
   run_at = "2026-03-25 09:00"

2. Write:

[
  {
    "prompt": "查看比特币价格",
    "run_at": "2026-03-25 09:00",
    "done": false
  }
]
