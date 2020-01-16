function registerPorts(ports) {
  if (ports.login) {
    ports.login.subscribe(function (creds) {

      console.log('Performing login with', creds);
      setTimeout(function () {
        if (creds.email === "ilya")
          ports.onLoginSuccess.send({token: "myToken"});
        else
          ports.onLoginError.send({errorMessage: "some error reason"});
      }, 2000);
    });
  }


  if (ports.loadBoards) {
    ports.loadBoards.subscribe(function (userToken) {

      console.log('Loading boards for ', userToken);

      if (userToken === "myToken") {

        setTimeout(function () {
          ports.onBoardsResponse.send(boardsModelResponse);
        }, 2000);
      }
    });
  }
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
