[In english](https://github.com/ciukstar/medcab/blob/master/README.md)  

[En français](https://github.com/ciukstar/medcab/blob/master/README.fr.md)  

[На русском](https://github.com/ciukstar/medcab/blob/master/README.ru.md)  


## MedCab
Aplicație web pentru sănătate

## Prezentare generală
Aplicația [MedCab](https://medcabro-jjgwe5ufda-de.a.run.app) oferă posibilitatea de a înregistra și urmări semnele vitale, de a vedea dinamica acestora în timp și de a le compara cu valorile normale.


## Consultație online medic-pacient
Dacă este necesar, pacientul poate alege un medic pe care să-l consulte prin schimb de mesaje (chat cu medicul) sau prin apel video/audio.

Chatul cu un medic este implementat folosind protocolul [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API).

Apelul video/audio cu medicul este implementat folosind [API-ul WebRTC](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API).

* Configurare
  * ```$YESOD_RTC_PEER_CONNECTION_CONFIG```
  
Pentru notificări se folosește [API-ul Web Push](https://developer.mozilla.org/en-US/docs/Web/API/Push_API).

## Entități de bază

### Utilizator

Un utilizator nou se poate înscrie folosind un cont Google sau creând un cont nou pe [pagina de autentificare](https://medcabro-jjgwe5ufda-de.a.run.app/auth/login).

Pentru a crea un cont nou, este necesar să furnizați o adresă de e-mail. Adresa de e-mail va fi folosită pentru a verifica contul și pentru a seta sau recupera parola.

Un superutilizator (vezi mai jos) poate acorda privilegii de administrator oricărui utilizator înregistrat. Un utilizator cu rol de administrator poate, la rândul său, să acorde sau să revoce rolul de administrator altor utilizatori înregistrați. Privilegiile de administrator sunt necesare pentru a gestiona datele la nivel de sistem.

### Unitate de măsură

O unitate de măsură este definită prin furnizarea unui nume, simbol și descriere în secțiunea [„Unități de măsură”](https://medcabro-jjgwe5ufda-de.a.run.app/data/units).

### Doctor

...

### Specialitatea

...

## Superutilizator

* Nume de utilizator  
  ```$YESOD_SUPERUSER_USERNAME```
* Parola  
  ```$YESOD_SUPERUSER_PASSWORD```
  
Un cont de superutilizator este definit în momentul implementării. Superutilizatorul gestionează alți utilizatori și acordă sau revocă privilegii de administrator anumitor utilizatori.

## Integrare cu API-uri externe

* E-mail: [Gmail API](https://developers.google.com/gmail/api/guides)  

  * Id-ul clientului  
    ```$YESOD_GOOGLE_CLIENT_ID```
  * Secretul clientului  
    ```$YESOD_GOOGLE_CLIENT_SECRET```

## Optimizare motor de căutare

* [Google SEO](https://search.google.com/search-console)

  ```$YESOD_GOOGLE_SITE_VERIFICATION```
  
* [Bing SEO](https://www.bing.com/webmasters)

  ```$YESOD_MS_VALIDATE```
  
* [Yandex SEO](https://webmaster.yandex.com/welcome)

  ```$YESOD_YANDEX_VERIFICATION```

## Diagrama ER

![Diagrama entitate - relație](static/img/ERD_MedCab.svg)

## Demo

[Click aici pentru a vedea demo](https://medcabro-jjgwe5ufda-de.a.run.app)

_* Faceți clic pe butonul [![Conturi de utilizator demonstrative](demo/button-demo-aaccounts.png)](https://medcabro-jjgwe5ufda-de.a.run.app/auth/login) pentru a obține o listă de conturi de utilizator demonstrative_
