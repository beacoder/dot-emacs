---
name: schedule-task
description: Task scheduling rules for reminders and delayed execution
---

# Task Scheduling Rules

## Trigger Conditions
When user asks about:
- reminders
- scheduling
- run later
- 定时执行 / 稍后执行 / 定时任务 / 提醒

You MUST create a scheduled task.

## Step 1: Extract Task
You MUST extract:

- prompt: a clear, concise task description (NO time info inside)
- run_at: absolute time in format "YYYY-MM-DD HH:MM"
- done: false
- repeat: recurrence rule (nullable, see below)

## Step 2: Time Rules (STRICT)

- ALWAYS convert relative time to absolute time
- Use system current time as reference
- Use 24-hour format
- Format MUST be exactly:

YYYY-MM-DD HH:MM

Examples:
- 明天早上9点 → 2026-03-25 09:00
- 10分钟后 → (current time + 10 minutes)

## Step 3: Repeat Rules
If user specifies repetition (每天 / 每周 / 每月 / every N minutes / every N hours), you MUST:
- Add "repeat" field
- Use structured values:

Type | Value | Example
Daily | "daily" | 每天
Weekly | "weekly:1" | 每周一 (1=Monday)
Monthly | "monthly:15" | 每月15号
Interval (minutes) | "interval:10m" | 每10分钟
Interval (hours) | "interval:2h" | 每2小时

- If the task is one-time, set "repeat": null
- "run_at" must always be the next occurrence

## Step 4: File Operation (STRICT)
Target file: ~/agent/schedule.json

1. If file exists:
   - Read file
   - If empty → treat as []
   - Parse JSON array

2. If file does NOT exist:
   - Initialize as empty array []

3. Append new task object

4. Write FULL JSON array back to file

## Step 5: JSON Format (STRICT)
Final file MUST be a VALID JSON ARRAY:

[
  {
    "prompt": "...",
    "run_at": "YYYY-MM-DD HH:MM",
    "done": false,
    "repeat": null
  }
]

- NEVER overwrite existing tasks
- NEVER write partial JSON
- NEVER write invalid JSON
- NEVER include comments
- NEVER include trailing commas
- NEVER include markdown or explanation

## Step 6: Tool Usage
- Read → to read existing file
- Write → to write updated JSON

## Step 7: Behavior Rules

- DO NOT execute the task immediately
- DO NOT call external tools for execution
- ONLY schedule

## Step 8: Examples

1. One-time task
User:
明天早上9点提醒我查看比特币价格

[
  {
    "prompt": "查看比特币价格",
    "run_at": "2026-03-25 09:00",
    "done": false,
    "repeat": null
  }
]

2. Daily recurring task
User:
每天早上7点喝水提醒

[
  {
    "prompt": "喝水提醒",
    "run_at": "2026-03-25 07:00",
    "done": false,
    "repeat": "daily"
  }
]

3. Weekly recurring task
User:
每周一下午3点团队会议提醒

[
  {
    "prompt": "团队会议提醒",
    "run_at": "2026-03-30 15:00",
    "done": false,
    "repeat": "weekly:1"
  }
]

4. Monthly recurring task
User:
每月15号发工资提醒

[
  {
    "prompt": "发工资提醒",
    "run_at": "2026-04-15 09:00",
    "done": false,
    "repeat": "monthly:15"
  }
]

5. Interval-based recurring task
User:
每10分钟检查一次服务器状态

[
  {
    "prompt": "检查服务器状态",
    "run_at": "2026-03-25 06:30",
    "done": false,
    "repeat": "interval:10m"
  }
]

Every 2 hours:

[
  {
    "prompt": "检查日志",
    "run_at": "2026-03-25 06:30",
    "done": false,
    "repeat": "interval:2h"
  }
]
