
[En français](https://github.com/ciukstar/medcab/blob/master/README.fr.md)  

[În română](https://github.com/ciukstar/medcab/blob/master/README.ro.md)  

[На русском](https://github.com/ciukstar/medcab/blob/master/README.ru.md)  

## MedCab
Healthcare Web App

## Overview
The app [MedCab](https://medcab-jjgwe5ufda-de.a.run.app) offers the ability to record and track vital signs, see their dynamics over time and compare them with normal values.


## Online Doctor-Patient consultation
If necessary, the patient can choose a doctor to consult by exchanging messages (chat with the doctor) or by video/audio call.

Chat with a doctor is implemented using the [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API) protocol.

Video/audio call with doctor is implemented using [WebRTC API](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API).

* Configuration
  * ```$YESOD_RTC_PEER_CONNECTION_CONFIG```
  
For notifications the [Web Push API](https://developer.mozilla.org/en-US/docs/Web/API/Push_API) is used.

## Basic Entities

### User

A new user can sign up using a Google account or by creating a new account on the [authentication page](https://medcab-jjgwe5ufda-de.a.run.app/auth/login).

To create a new account, it is necessary to provide an email address. The email address will be used to verify the account and to set or recover the password.

A superuser (see below) can grant administrator privileges to any registered user. A user with the administrator role can in turn grant or revoke the administrator role to other registered users. Administrator privileges are required to manage system-wide data.

### Unit of measurement

A unit of measurement is defined by providing a name, symbol, and description in the section [“Measurement units”](https://medcab-jjgwe5ufda-de.a.run.app/data/units).

### Doctor

A doctor must be registered by an administrator in the subsection ["Doctors"](https://medcab-jjgwe5ufda-de.a.run.app/data/staff) under the menu entry "Data" and a corresponding user account of the new doctor can be assigned to him.

A doctor can designate users as patients by adding them to the patient list.

### Patient

A patient is a user that a doctor has designated as his or her patient.

### Specialty

...

## Superuser

* Username  
  ```$YESOD_SUPERUSER_USERNAME```
* Password  
  ```$YESOD_SUPERUSER_PASSWORD```
  
A superuser account is defined at deployment time. The superuser manages other users and grants or revokes administrator privileges to specific users.

## Integration with external APIs

* Email: [Gmail API](https://developers.google.com/gmail/api/guides)  

  * Client id  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Client secret  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

## Search Engine Optimization

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```
  
* [Yandex SEO](https://webmaster.yandex.com/welcome)

  ```$YESOD_YANDEX_VERIFICATION```

## ER Diagram

![Entity Relationship Diagram](static/img/ERD_MedCab.svg)

## Demo

[Click here to see demo](https://medcab-jjgwe5ufda-de.a.run.app)

_* Click on the [![Demo user accounts](demo/button-demo-aaccounts.png)](https://medcab-jjgwe5ufda-de.a.run.app/auth/login) button to get a list of demo accounts_
