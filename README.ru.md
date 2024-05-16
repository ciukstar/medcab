
[In english](https://github.com/ciukstar/medcab/blob/master/README.md)  

[En français](https://github.com/ciukstar/medcab/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/medcab/blob/master/README.ro.md)  

## MedCab
Веб-приложение для здравоохранения

## Обзор
Приложение [MedCab](https://medcabru-jjgwe5ufda-de.a.run.app) предлагает возможность записывать и отслеживать показатели жизнедеятельности, видеть их динамику с течением времени и сравнивать их с нормальными значениями.


## Онлайн-консультация врача и пациента
При необходимости пациент может выбрать врача для консультации путем обмена сообщениями (чата с врачом) или посредством видео/аудио звонка.

Чат с врачом реализован по протоколу [WebSockets](https://developer.mozilla.org/ru/docs/Web/API/WebSockets_API).

Видео/аудиосвязь с врачом реализована с помощью [WebRTC API](https://developer.mozilla.org/ru/docs/Web/API/WebRTC_API).

* Конфигурация
  * ```$YESOD_RTC_PEER_CONNECTION_CONFIG```
  
Для уведомлений используется [Web Push API](https://developer.mozilla.org/ru/docs/Web/API/Push_API).

## Основные сущности

### Пользователь

Новый пользователь может зарегистрироваться, используя учетную запись Google или создав новую учетную запись на [странице аутентификации](https://medcabru-jjgwe5ufda-de.a.run.app/auth/login).

Для создания новой учетной записи необходимо указать адрес электронной почты. Адрес электронной почты будет использоваться для проверки учетной записи, а также для установки или восстановления пароля.

Суперпользователь (см. ниже) может предоставить права администратора любому зарегистрированному пользователю. Пользователь с ролью администратора, в свою очередь, может предоставить или отозвать роль администратора другим зарегистрированным пользователям. Для управления общесистемными данными необходимы права администратора.

### Единица измерения

Единица измерения определяется путем указания имени, символа и описания в разделе [«Единицы измерения»](https://medcabru-jjgwe5ufda-de.a.run.app/data/units).

### Врач

...

### Специальность

...

## Суперпользователь

* Имя пользователя  
  ```$YESOD_SUPERUSER_USERNAME```
* Пароль  
  ```$YESOD_SUPERUSER_PASSWORD```
  
Учетная запись суперпользователя определяется во время развертывания. Суперпользователь управляет другими пользователями и предоставляет или отзывает права администратора конкретным пользователям.

## Интеграция с внешними API

* Электронная почта: [Gmail API](https://developers.google.com/gmail/api/guides)  

  * Идентификатор клиента  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Секрет клиента  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

## Поисковая оптимизация

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```
  
* [Yandex SEO](https://webmaster.yandex.com/welcome)

  ```$YESOD_YANDEX_VERIFICATION```

## ER-диаграмма

![Диаграмма отношений сущностей](static/img/ERD_MedCab.svg)

## Демо

[Нажмите здесь, чтобы увидеть демо](https://medcabru-jjgwe5ufda-de.a.run.app)

_* Нажмите на кнопку [![Демо-аккаунты пользователей](demo/button-demo-aaccounts.png)](https://medcabru-jjgwe5ufda-de.a.run.app/auth/login), чтобы получить список демонстрационных учетных записей пользователей_