
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
      var title, body;
      if (message.messageType === 'PushMsgTypeDecline') {
	title = 'Incoming call declined';
	body = `Call from user: ${message.senderName} declined`;
      } else if (message.messageType === 'PushMsgTypeAccept') {
	title = 'Incoming call accepted';
	body = `Call from user: ${message.senderName} accepted`;
      } else if (message.messageType === 'PushMsgTypeCancel') {
	title = 'Call Canceled';
	body = 'Call Canceled';
      } else if (message.messageType === 'PushMsgTypeEndVideoSession') {
	title = 'PushMsgTypeEndVideoSession';
	body = 'PushMsgTypeEndVideoSession';
      } else if (message.messageType === 'PushMsgTypeCall') {
	title = 'Incoming call';
	body = `Call from user: ${message.senderName}`;
      }
      
      return self.registration.showNotification(title, {
	image: message.senderPhoto,
	icon: message.icon,
        body: body
      });
    })
  );
};
