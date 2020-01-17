function registerPorts(ports) {
  const auth = firebase.auth();
  const provider = new firebase.auth.GoogleAuthProvider();

  auth.onAuthStateChanged(function (user) {
    if (user)
      callPort(ports, 'onLogin', user);
    else
      callPort(ports, 'onLogout', "ignored value");
  });
  subscribeToPort(ports, 'googleSignin', function () {
    auth.signInWithPopup(provider);
  });


  if (ports.loadBoards) {
    ports.loadBoards.subscribe(function (userToken) {
      setTimeout(function () {
        ports.onBoardsResponse.send(boardsModelResponse);
      }, 200);
    });
  }
}


function subscribeToPort(app, portName, handler) {
  if (!app[portName]) {
    console.error('Port ' + portName + " doesn't exist on ", app);
  } else
    app[portName].subscribe(handler);
}

function callPort(app, portName, data) {
  if (!app[portName]) {
    console.error('Port ' + portName + " doesn't exist on ", app);
  } else
    app[portName].send(data);
}


const boardsModelResponse = {
  selectedBoard: "BOARD_1",
  boards: [{
    id: "BOARD_1",
    name: "First Board",
    stacks: [
      {
        id: "STACK_1",
        name: "FirstStack",
        items: [
          {
            id: "ITEM_1",
            name: "first item",
            youtubeId: "WddpRmmAYkg",
          },
        ],
      },
    ],
  }],
};
