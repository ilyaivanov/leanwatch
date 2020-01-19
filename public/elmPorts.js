const auth = firebase.auth();
const firestore = firebase.firestore();

function registerPorts(ports) {
  const provider = new firebase.auth.GoogleAuthProvider();

  auth.onAuthStateChanged(function (user) {
    if (user) {
      ports.onLogin.send(user);
      handleUserLogin(user, userProfile => {
        ports.onUserProfileLoaded.send(userProfile);
        firestore.collection('boards').where('id', 'in', userProfile.boards).get().then(snapshot => {
          ports.onBoardsLoaded.send(snapshot.docs.map(d => d.data()));
        });
      });
    } else
      ports.onLogout.send("ignored value");
  });

  ports.googleSignin.subscribe(() => auth.signInWithPopup(provider));

  ports.saveBoard.subscribe(function (board) {
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
      newBoard.set({...defaultBoard, id: newBoard.id});
      const newProfile = {
        boards: [newBoard.id],
        selectedBoard: newBoard.id,
      };
      userRef.set(newProfile);
      onSuccess({id: userRef.id, ...newProfile});
    }
  });
}

// TODO: move default boards to the backend
// So that you can assign unique ids during creation
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
