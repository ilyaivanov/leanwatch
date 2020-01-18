function registerPorts(ports) {
  setTimeout(() => {
    console.log(ports);
    ports.onLogin.send({
      displayName: "Dummy user"
      , photoURL: "https://dg6xfr3y1xvv2.cloudfront.net/nicole-lavelle-smiley-face-sticker-MAIN-5c33f921db820-1500.jpg"
      , email: "42",
    });
    ports.onUserProfileLoaded.send({
      selectedBoard: "BOARD_1",
      boards: [
        {id: 'BOARD_1', name: "First Board"},
        {id: 'BOARD_2', name: "Second Board"},
        {id: 'BOARD_3', name: "Third Board"},

      ],
    });
  }, 200);

  ports.loadBoard.subscribe(function (boardId) {
    ports.onBoardLoaded.send(boards[boardId]);
  });

  ports.saveBoard.subscribe(function (board) {
    console.log('saving board', board);
  });
}

const boards = {
  "BOARD_1": {...createDummyBoard()},
  "BOARD_2": {...createDummyBoard(), id: "BOARD_2", name:"Second Board"},
  "BOARD_3": {...createDummyBoard(), id: "BOARD_3", name:"Third Board"},
};

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
