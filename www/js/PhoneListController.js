app.controller('PhoneListCtrl',
           ['$scope', '$http',
    function($scope ,  $http) {

    $scope.phones = [];
    $scope.isCollapsed = false
    $http({method: 'GET', url: '/api/phone'})
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
        if (!number) {
            number = (Math.random()+"").substr(5, 5);
        }
        $scope.phones.push({number: number, callNumber: ""});
        $scope.addPhone = "";
    }
}]);
