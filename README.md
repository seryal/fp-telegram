# fp-telegram #

Library for working with Telegram bots API in FreePascal/Lazarus

FreePascal wrapper classes for Telegram bot's API. You can use it in your own working projects. Please join if you want to help of the development this library.

The library **does not use any third-party libraries**, besides the built-in FPC. You can use it both independently and together with other libraries, for example, as a plug-in to Brook-framework https://github.com/Al-Muhandis/brook-telegram/

Done:
+ webhook getting updates
+ longpolling getting udates via getUpdates API method
+ Telegram bots API methods implemented:
  + getMe
  + sendMessage
  + sendPhoto
  + sendVideo
  + getUpdates
  + answerInlineQuery
  + sendLocation
+ Update events handling
  + All implemented    
+ Full json updates logging (without handling)
+ Simple statistcs (csv-format)

Todo:
+ Extensive statistics *(Quantitative data on requests and unique users)*
+ Other bots API methods
+ ~~Other update events handling~~
+ Please suggest me other functionalities

***

Библиотека для работы с API ботов в телеграмм

FreePascal библиотека классов для работы с API телеграм-ботов. Вы уже можете использовать его в своих рабочих проектах. Если Вы хотите помочь в развитии проекта - присоединяйтесь!

Библиотека **не использует никаких сторонних библиотек**, кроме встроенных в FPC. Вы можете использовать ее как самостоятельно, так и в составе других библиотек, к примеру, как плагин к Brook-framework https://github.com/Al-Muhandis/brook-telegram/


