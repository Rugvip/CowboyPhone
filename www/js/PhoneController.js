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

    phone.lastAction = "";

    function setState(state) {
        phone.state = state;

        phone.showPad = !!numpadShown[state];
    }

    $scope.activate = function () {
        phone.state = "connecting";

        var ws = openWebSocket($scope, {
            actions: {
                inbound: function (number) {
                    phone.lastAction = "inbound from " + number;
                },
                accept: function () {
                    phone.lastAction = "accept";
                },
                hangup: function () {
                    phone.lastAction = "hangup";
                },
                closed: function () {
                    setState('none');
                },
                deleted: function () {
                    setState('none');
                    $scope.remove($scope.phone);
                }
            },
            data: function (data) {
                console.log("got data: " + data);
            },
            state: setState
        });

        $scope.call = function (number) {
            console.log("Calling " + number);
            if (phone.state === 'connected') {
                ws.data("HI DAWG " + number);
            } else {
                ws.action({call: number});
            }
        }

        $scope.accept = function () {ws.action('accept'); }
        $scope.reject = function () {ws.action('reject'); }
        $scope.hangup = function () {ws.action('hangup'); }
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


function openWebSocket($scope, callbacks) {
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
            if (data.action && callbacks.actions) {
                if (typeof data.action === 'object') {
                    for (key in data.action) {
                        callbacks.actions[key] && callbacks.actions[key](data.action[key]);
                    }
                } else {
                    callbacks.actions[data.action] && callbacks.actions[data.action]();
                }
            }
            if (data.state && callbacks.state) {
                callbacks.state(data.state);
            }
        });

        if (data && data.data && callbacks.data) {
            callbacks.data(data.data);
        }
    };

    ws.onclose = function() {
        $scope.$apply(function () {
            callbacks.actions.closed();
        });
    };

    return {
        action: function (action) {
            console.log("Sending action ", action);
            ws.send(JSON.stringify({action: action}));
        },
        data: function (data) {
            console.log("Sending data ", data);
            ws.send(JSON.stringify({data: data}));
        }
    };
}
