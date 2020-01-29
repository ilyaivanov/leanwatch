const auth = firebase.auth();
const db = firebase.firestore();

function registerFirebase(ports) {
  const provider = new firebase.auth.GoogleAuthProvider();

  auth.onAuthStateChanged(function (user) {
    if (user) {
      ports.onLogin.send(user);
      handleUserLogin(user, userProfile => {
        ports.onUserProfileLoaded.send(userProfile);
        db.collection('boards').where('id', 'in', userProfile.boards).get().then(snapshot => {
          ports.onBoardsLoaded.send(snapshot.docs.map(d => d.data()));
        });
      });
    } else
      ports.onLogout.send("ignored value");
  });

  ports.googleSignin.subscribe(() => auth.signInWithPopup(provider).then().catch(res => {
    ports.onLoginCancel.send(null);
  }));
  ports.logout.subscribe(() => auth.signOut());

  ports.saveBoard.subscribe(function (boards) {
    if (boards.length > 0) {
      const batch = db.batch();
      boards.forEach(board => {
        var ref = db.collection("boards").doc(board.id);
        batch.set(ref, board);
      });
      batch.commit().then(function () {
        console.log('Saved ' + boards.length + ' boards. (' + boards.map(b => b.name).join(', ') + ')');
      });
    }
  });

  ports.saveProfile.subscribe(function (userProfile) {
    db.collection('users').doc(userProfile.id).set(userProfile).then(() => {
      console.log('Saved user profile', userProfile);
    });
  });

  ports.createBoard.subscribe(function () {
    const ref = db.collection('boards').doc();
    ports.onBoardCreated.send({id: ref.id, name: "First Board", stacks: []});
  });

  ports.scrollItemToBeginning.subscribe(function (elementId) {
    const elememt = document.getElementById(elementId);
    if (elememt) {
      elememt.scrollTo({left: 0, top: 0, behavior: "smooth"});
    } else {
      console.error("Can't find element with id " + elementId);
    }
  });

}

function handleUserLogin(user, onSuccess) {
  const userRef = db.doc('users/' + user.uid);
  userRef.get().then(function (snapshot) {
    if (snapshot.exists) {
      const profile = {id: snapshot.id, ...snapshot.data()};
      console.log('Received profile ', profile);
      onSuccess(profile);
    } else {
      const newBoard = db.collection('boards').doc();
      console.log('Creating board', {...defaultBoard, id: newBoard.id});
      newBoard.set({...defaultBoard, id: newBoard.id});
      const newProfile = {
        id: userRef.id,
        boards: [newBoard.id],
        selectedBoard: newBoard.id,
        syncTime: 1000 * 30,
      };
      console.log('Created profile ', newProfile);
      userRef.set(newProfile);
      onSuccess({id: userRef.id, ...newProfile});
    }
  });
}

// TODO: move default board creation to the backend
// So that you can assign unique ids for items during creation
// now this isn't a problem, but in the future you might share initial board
// and you can move item from the initial board to your initial board. This way you will have two items with the same id
// Probability of this is super minimal
const defaultBoard = {
  name: "Your First Board",
  stacks: [
    {
      id: "STACK_1",
      name: "Try dragging other stuff here",
      items: [
        {
          id: "ITEM_1",
          name: "You can create other columns and boards and find videos using 'Search'",
          youtubeId: "WddpRmmAYkg",
        },
      ],
    },
  ],
};
