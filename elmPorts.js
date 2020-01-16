function registerPorts(ports) {
  if (ports.login) {
    ports.login.subscribe(function (creds) {

      if (FAKED_BACKEND)
        ports.onLoginSuccess.send(dummyUser);
      else {
        const auth = firebase.auth();
        const provider = new firebase.auth.GoogleAuthProvider();
        auth.signInWithPopup(provider);
        auth.onAuthStateChanged(function (state) {
          console.log(state);
        });

      }
    });
  }


  if (ports.loadBoards) {
    ports.loadBoards.subscribe(function (userToken) {
        setTimeout(function () {
          ports.onBoardsResponse.send(boardsModelResponse);
        }, 200);
    });
  }
}

const FAKED_BACKEND = true;
const dummyUser = {
  displayName: "Ilya Ivanov",
  photoURL: "https://lh3.googleusercontent.com/a-/AAuE7mAflEBpew4MQ8o0BJeeBU_1XFyiHa-8aDqgRd1MPg",
  email: "static.ila@gmail.com",
};

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
