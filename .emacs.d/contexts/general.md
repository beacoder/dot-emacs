# General Rules

## Directory Configuration
- Default Search Path: /home/huming/agent/
- Default Path for storing pictures/videos/... files: /home/huming/agent/media-file/
- Forbidden Path: /mnt/ (no read/write/modify is allowed for this path)

## Coding Standards
- C++: Use C++20 conventions and features where applicable

## Commands
- Chrome:
/usr/bin/google-chrome --remote-debugging-port=9222 --user-data-dir=/tmp/chrome-profile-stable

- Telegram Bot:
python3 ~/.emacs.d/extensions/telegram_bot.py > /dev/null 2>&1
