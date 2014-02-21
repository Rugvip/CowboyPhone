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
    phone.remote = "";

    phone.messages = [];

    function setState(state) {
        phone.state = state;

        phone.showPad = !!numpadShown[state];

        if (state === 'idle') {
            phone.remote = "";
        }
    }

    $scope.connect = function () {
        phone.state = "connecting";

        var peer;

        var ws = openWebSocket($scope, {
            actions: {
                inbound: function (number) {
                    phone.remote = number;
                },
                accept: function () {
                    // navigator.webkitGetUserMedia({audio: true}, function(stream) {
                    //     peer = RTCPeerConnection({
                    //         attachStream: stream,

                    //         onICE: function (candidate) {
                    //             ws.data({ice: candidate});
                    //         },
                    //         onRemoteStream: function (str) {
                    //             console.log("STREAM ASDASD");
                    //             aud.src = URL.createObjectURL(str);
                    //             aud.play();
                    //         },

                    //         onOfferSDP: function (offerSDP) {
                    //             console.log("maek offer");
                    //             ws.data({offer: offerSDP});
                    //         },

                    //         onChannelMessage: function (event) {
                    //             console.log("Channel message: ", event);
                    //         },
                    //         onChannelOpened: function (_RTCDataChannel) {
                    //             console.log("we has data channel");
                    //         }
                    //     });
                    // });
                },
                hangup: function () {
                },
                deleted: function () {
                    setState('none');
                    $scope.remove($scope.phone);
                }
            },
            closed: function () {
                setState('none');
            },
            data: function (data) {
                console.log("got data: " + data);
                if (data.message) {
                    $scope.$apply(function () {
                        phone.messages.push({
                            content: data.message,
                            side: 'right'
                        });
                    });
                }
                // if (data.offer) {
                //     navigator.webkitGetUserMedia({audio: true}, function(stream) {
                //         var peer = RTCPeerConnection({
                //             attachStream: stream,

                //             offerSDP: data.offer,

                //             onICE: function (candidate) {
                //                 ws.data({ice: candidate});
                //             },
                //             onRemoteStream: function (str) {
                //                 console.log("STREAM ASD");
                //                 aud.src = URL.createObjectURL(str);
                //                 aud.play();
                //             },

                //             onAnswerSDP: function (answerSDP) {
                //                 console.log("maek anwser");
                //                 ws.data({answer: answerSDP});
                //             },

                //             onChannelMessage: function (event) {
                //                 console.log("Channel message: ", event);
                //             },
                //             onChannelOpened: function (_RTCDataChannel) {
                //                 console.log("we has data channel");
                //             }
                //         });
                //     });
                // }
                // if (data.ice) {
                //     peer.addICE({
                //         sdpMLineIndex: data.ice.sdpMLineIndex,
                //         candidate: data.ice.candidate
                //     });
                // }
                // if (data.answer) {
                //     peer.addAnswerSDP(data.answer);
                // }
            },
            state: setState
        });

        $scope.call = function (number) {
            console.log("Calling " + number);

            phone.remote = number;
            ws.action({call: number});
        }

        $scope.disconnect = function () {
            ws.action('disconnect');
        };

        $scope.send = function (message) {
            if (phone.state === 'connected') {
                phone.messages.push({
                    content: message,
                    side: 'left'
                });
            }
            ws.data({message: message});
        };

        $scope.accept = function () {ws.action('accept'); }
        $scope.reject = function () {ws.action('reject'); }
        $scope.hangup = function () {ws.action('hangup'); }
        $scope.disconnect = function () {ws.action('disconnect'); }
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

    var ws = new WebSocket("ws://" + location.host + "/sock/" + number);

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
            callbacks.closed && callbacks.closed();
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
