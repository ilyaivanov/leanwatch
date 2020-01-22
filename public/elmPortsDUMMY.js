function registerPorts(ports) {
  setTimeout(() => {
    console.log(ports);
    ports.onLogin.send({
      displayName: "Dummy user"
      , photoURL: "https://dg6xfr3y1xvv2.cloudfront.net/nicole-lavelle-smiley-face-sticker-MAIN-5c33f921db820-1500.jpg"
      , email: "42",
    });
    ports.onUserProfileLoaded.send({
      id: "USER_1",
      selectedBoard: "BOARD_1",
      boards: ['BOARD_1', 'BOARD_2', 'BOARD_3'],
    });

    setTimeout(() => {
      ports.onBoardsLoaded.send(boards);
    }, 200);
  }, 200);

  ports.saveBoard.subscribe(function (board) {
    console.log('saving board', board);
  });

  ports.saveProfile.subscribe(function (userProfile) {
    console.log('saving userProfile', userProfile);
  });

  ports.createBoard.subscribe(function () {
    ports.onBoardCreated.send({id: Math.random() + "", name: "New Board", stacks: []});
  });

}

const boards = [
  {...createDummyBoard()},
  {...createDummyBoard(), id: "BOARD_2", name: "Second Board"},
  {...createDummyBoard(), id: "BOARD_3", name: "Third Board"},
];


function createDummyBoard() {
  return {
    id: 'BOARD_1',
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
}
