
self.addEventListener('push', function (event) {

  const message = event.data.json();
  
  event.waitUntil(
    self.clients.matchAll({ type: 'window' }).then(function (clients) {
      if (clients.length > 0) {
        for (const client of clients) {
          client.postMessage(message);
        }
      }
      return message;
    }).then(function (message) {
      return self.registration.showNotification("Call from user: " + message.senderName, {
        body: message
      });
    })
  );
});
