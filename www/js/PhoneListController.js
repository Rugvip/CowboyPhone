app.controller('PhoneListCtrl',
           ['$scope', '$http',
    function($scope ,  $http) {

    $scope.phones = [];
    $scope.isCollapsed = false
    $http.get('/api/phone')
        .success(function (data, status, headers) {
            console.log(data);
            $scope.phones = data;
        })
        .error(function (data, status) {
            alert("Phone fetch failed: " + status);
        });

    $scope.remove = function (phone) {
        $scope.phones.splice($scope.phones.indexOf(phone), 1);
    };
    $scope.add = function (number) {
        $http.post('/api/phone', {number: number}, {
            headers: {
                'Content-Type': 'application/json',
                'Accept': 'application/json'
            }
        })
            .success(function (data, status, headers) {
                console.log(data);
                $scope.phones = data;
            })
            .error(function (data, status) {
                alert("Phone fetch failed: " + status);
            });
        if (!number) {
            number = (Math.random()+"").substr(5, 5);
        }
        // $scope.phones.push({number: number, callNumber: ""});
        $scope.addPhone = "";
    }
}]);
