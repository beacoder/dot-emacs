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
