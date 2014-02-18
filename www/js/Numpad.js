app.directive('numpad', function () {
    return {
        restrict: 'E',
        templateUrl: 'html/numpad.html',
        scope: {
            onNumber: '&'
        },
        controller: ['$scope', function ($scope) {
            $scope.input = "";
            $scope.numpad = [[7,8,9],[4,5,6],[1,2,3],['clear', 0, 'del']];

            var inputs = {
                clear: function () {
                    $scope.input = "";
                },
                del: function () {
                    $scope.input = $scope.input.slice(0, -1);
                }
            };

            $scope.click = function (n) {
                if (inputs[n]) {
                    inputs[n]();
                } else {
                    $scope.input += n;
                }
            };

            $scope.commit = function () {
                if ($scope.input.length) {
                    $scope.onNumber({number: $scope.input});
                    $scope.input = "";
                }
            };

            $scope.keydown = function (e) {
                if (e.keyCode === 13) {
                    $scope.onNumber({number: $scope.input});
                    $scope.input = "";
                }
            }
        }]
    };
});
