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
      syncTime: 1000 * 60,
    });

    setTimeout(() => {
      ports.onBoardsLoaded.send(boards);
    }, 200);
  }, 200);

  ports.logout.subscribe(() => ports.onLogout.send("ignored value"));


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
  {...createDummyBoard(), id: "BOARD_2", name: "FAKE BOARD"},
  {...createDummyBoard(), id: "BOARD_3", name: "FAKE BOARD1"},
];


function createDummyBoard() {
  return {
    id: 'BOARD_1',
    name: "FAKE BOARD",
    stacks: [
      {
        id: "STACK_1",
        name: "FirstStack",
        items: [
          {
            id: "ITEM_1",
            name: "The very vest of  item",
            youtubeId: "WddpRmmAYkg",
          },
          {
            id: "ITEM_2",
            name: "The very vest of  item",
            youtubeId: "nMJn2A8PDQA",
          },
        ],
      }, {
        id: "STACK_2",
        name: "FirstStack",
        items: [
          {
            id: "ITEM_3",
            name: "The very vest of  item",
            youtubeId: "WddpRmmAYkg",
          },
          {
            id: "ITEM_4",
            name: "The very vest of  item",
            youtubeId: "nMJn2A8PDQA",
          },
        ],
      },
    ],
  };
}
