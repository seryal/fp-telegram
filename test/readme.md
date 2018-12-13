Add some data so that you can test the telegram bots API wrapper
``` INI
[Bot]
;; Please specify bot token below!
Token=
[API]
;; Do not pay attention to this - it is needs for countries where telegram is blocked. (You can specify custom telegram endpoint)
;; Endpoint=
[Chat]
;; Enter Your user/chat ID
ID=
```
In order to test receiving updates via longpolling, send a message or other \[action\] to the bot just before testing (procedure `TTestReceiveLongPolling.Receive`).
