const auth = firebase.auth();
const firestore = firebase.firestore();

function registerPorts(ports) {

  const provider = new firebase.auth.GoogleAuthProvider();

  auth.onAuthStateChanged(function (user) {
    if (user) {
      callPort(ports, 'onLogin', user);
      handleUserLogin(user, userProfile => ports.onUserProfileLoaded.send(userProfile));
    } else
      callPort(ports, 'onLogout', "ignored value");
  });
  subscribeToPort(ports, 'googleSignin', function () {
    auth.signInWithPopup(provider);
  });
}

function handleUserLogin(user, onSuccess) {
  const userRef = firestore.doc('users/' + user.uid);
  userRef.get().then(function (snapshot) {
    if (snapshot.exists) {
      onSuccess({id: snapshot.id, ...snapshot.data()});
    } else {
      const newBoard = firestore.collection('boards').doc();
      newBoard.set(defaultBoard);
      const newProfile = {
        boards: [{id: newBoard.id, name: defaultBoard.name}],
        selectedBoard: newBoard.id,
      };
      userRef.set(newProfile);
      onSuccess({id: userRef.id, ...newProfile});
    }
  });
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


const defaultBoard = {
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
};

const boardsModelResponse = {
  selectedBoard: "BOARD_1",
  boards: [{...defaultBoard, id: "TEMP ID"}],
};
