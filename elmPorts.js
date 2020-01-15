function registerPorts(ports) {
  ports.login.subscribe(function (creds) {

    console.log('Performing login with', creds);
    setTimeout(function () {
      if (creds.email === "ilya")
        ports.onLoginSuccess.send({foo: ""});
      else
        ports.onLoginError.send({foo: ""});
    }, 2000);
  });
}



