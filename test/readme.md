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

;; The section for payments tests
[Payment]

;; Get from the @BotFather
ProviderToken=
;; For example, USD or USD
Currency=USD
;; For example, $
PricePortionLabel=
;; for example: 10000, that is 100.00 [dollars/rub/etc]
PricePortionAmount=10000
```
In order to test receiving updates via longpolling, send a message or other \[action\] to the bot just before testing (procedure `TTestReceiveLongPolling.Receive`).
In order to test payments wiring, run sendInvoice test. Test the payment via telegram than run the ReceivePreCheckOutQuery
