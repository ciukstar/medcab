## App MedCab

[This application](https://medcab-jjgwe5ufda-de.a.run.app) is under development...

## Basic Entities

### User

....

### Doctor

...

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

## ER Diagram

![Entity Relationship Diagram](static/img/ERD_MedCab.svg)

## Demo

[Click here to see demo](https://medcab-jjgwe5ufda-de.a.run.app)

_* Click on the [![Demo user accounts](demo/button-demo-aaccounts.png)](https://medcab-jjgwe5ufda-de.a.run.app/auth/login) button to get a list of demo accounts_
