var numpadShown = {
    idle: true,
    receiving: true,
    calling: true,
    connected: true
};

app.controller('PhoneCtrl', ['$scope', function($scope) {
    var phone = $scope.phone;

    phone.state = 'none';
    phone.showPad = false;

    function setState(state) {
        phone.state = state;

        phone.showPad = !!numpadShown[state];
    }

    $scope.activate = function () {
        phone.state = "connecting";

        var ws = openWebSocket($scope, function (data) {
            if (!data) {
                setState('none');
                return;
            }

            if (data.state) {
                setState(data.state);
            }
        });

        $scope.call = function (number) {
            console.log("Calling " + number);
            ws.action({call: number});
        }

        $scope.accept = function () {
            console.log("Accept");
            ws.action('accept');
        }

        $scope.reject = function () {
            console.log("Reject");
            ws.action('reject');
        }
    };

    $scope.deactivate = function () {
        phone.active = false;
    };
}])
.directive('phone', function () {
    return {
        restrict: 'E',
        templateUrl: 'html/phone.html'
    };
});


function openWebSocket($scope, callback) {
    if (!("WebSocket" in window)) {
        alert("This browser does not support WebSockets");
        return;
    }

    var number = $scope.phone.number;

    var ws = new WebSocket("ws://localhost:8080/sock/" + number);

    ws.onopen = function() {
        console.log(number, " connected");
    };

    ws.onmessage = function (e) {
        var data = JSON.parse(e.data);
        console.log(number, " received: ", data);

        data && $scope.$apply(function () {
            callback(data);
        });
    };

    ws.onclose = function() {
        $scope.$apply(function () {
            callback(null);
        });
    };

    return {
        action: function (action) {
            ws.send(JSON.stringify({action: action}));
        }
    };
}
