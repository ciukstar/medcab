
self.addEventListener('push', function(event) {
  
  event.waitUntil(
    self.registration.showNotification("Call from user",{
      body: "A User is calling ..."
    })
  );
  
});
