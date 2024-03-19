
self.onpush = function (event) {

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
      const [title,body] = message.messageType == 'PushMsgTypeDecline'
	    ? ['Incoming call declined', `Call from user: ${message.senderName} declined`]
	    : ( message.messageType == 'PushMsgTypeAccept'
		? ['Incoming call accepted', `Call from user: ${message.senderName} accepted`]
		: ['Incoming call', `Call from user: ${message.senderName}`]
	      );
      return self.registration.showNotification(title, {
	image: message.senderPhoto,
	icon: message.icon,
        body: body
      });
    })
  );
};
