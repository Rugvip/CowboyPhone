var classes = {
    idle: 'label-default',
    calling: 'label-primary',
    receiving: 'label-warning',
    connected: 'label-success'
};

app.directive('chat', function () {
    return {
        restrict: 'E',
        templateUrl: 'html/chat.html',
        scope: {
            onSend: '&onSend',
            phone: '=phone'
        },
        controller: ['$scope', function ($scope) {
            $scope.input = "";

            $scope.commit = function () {
                if ($scope.input.length) {
                    $scope.onSend({message: $scope.input});
                    $scope.input = "";
                }
            };

            $scope.keydown = function (e) {
                if (e.keyCode === 13) {
                    $scope.onSend({message: $scope.input});
                    $scope.input = "";
                }
            }
        }]
    };
}).filter('stateClass', function () {
    return function (state) {
        return classes[state] || "";
    };
});
