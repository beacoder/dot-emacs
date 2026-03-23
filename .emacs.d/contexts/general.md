# General Rules

## Directory Configuration
- **Default Search Path**: `~/agent/` (used when no specific directory is provided)
- **Default Path for storing pictures/videos**: `~/agent/media-file/`
- **Forbidden Path**: `/mnt/` (no read/write/modify is allowed for this path)

## Coding Standards
- **C++**: Use C++20 conventions and features where applicable

## Command to start google-chrome
/usr/bin/google-chrome --remote-debugging-port=9222 --user-data-dir=/tmp/chrome-profile-stable

## Command to start telegram bot
python3 ~/.emacs.d/extensions/telegram_bot.py > /dev/null 2>&1
