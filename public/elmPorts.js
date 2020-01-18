const auth = firebase.auth();
const firestore = firebase.firestore();

function registerPorts(ports) {
  const provider = new firebase.auth.GoogleAuthProvider();

  auth.onAuthStateChanged(function (user) {
    if (user) {
      ports.onLogin.send(user);
      handleUserLogin(user, userProfile => ports.onUserProfileLoaded.send(userProfile));
    } else
      ports.onLogout.send("ignored value");
  });

  ports.googleSignin.subscribe(() => auth.signInWithPopup(provider));

  ports.loadBoard.subscribe(function (boardId) {
    firestore.collection('boards').doc(boardId).get().then(snapshot => {
      ports.onBoardLoaded.send({...snapshot.data(), id: snapshot.id});
    });
  });

  ports.saveBoard.subscribe(function (board) {
    console.log('saving board', board);
    firestore.collection('boards').doc(board.id).set(board).then(snapshot => {
      console.log('Board ' + board.name + " have been saved");
    });
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
