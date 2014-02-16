app.controller('PhoneCtrl', ['$scope', function($scope) {
    var phone = $scope.phone;

    phone.isShown = phone.id % 2 == 1
    phone.active = false;

    console.log($scope);
    phone.numpad = [[7,8,9],[4,5,6],[1,2,3],['clear', 0, 'del']];

    var inputs = {
        clear: function () {
            phone.callNumber = "";
        },
        del: function () {
            phone.callNumber = phone.callNumber.slice(0, -1);
        }
    };

    $scope.numpadClick = function (n) {
        if (inputs[n]) {
            inputs[n]();
        } else {
            phone.callNumber += n;
        }
    };

    $scope.call = function () {
        console.log("Calling " + phone.callNumber);
    }

    $scope.activate = function () {
        phone.active = true;
    };
}])
.directive('phone', function () {
    return {
        restrict: 'E',
        templateUrl: 'html/phone.html'
    };
});


/*(function () {
    var ws;

    send.onclick = function ()
    {
        ws.send("hello world!");
        console.log('Message sent');
    }

    openWs.onclick = function ()
    {
        console.log("asdasd");
        if (!("WebSocket" in window)) {
            alert("This browser does not support WebSockets");
            return;
        }
        ws = new WebSocket("ws://localhost:8080/sock");
        ws.onopen = function() {
            console.log('Connected');
        };
        ws.onmessage = function (evt)
        {
            var received_msg = evt.data;
            console.log("Received: " + received_msg);
            var txt = document.createTextNode("Simon says: " + received_msg );
            document.getElementById('msgs').appendChild(txt);
        };
        ws.onclose = function()
        {
            console.log('Connection closed');
        };
    }
}());*/
