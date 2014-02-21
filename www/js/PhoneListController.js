app.controller('PhoneListCtrl',
           ['$scope', '$http',
    function($scope ,  $http) {

    $scope.phones = [];
    $scope.showError = false;

    $http.get('/api/phone')
        .success(function (data, status, headers) {
            console.log(data);
            $scope.phones = data;
        })
        .error(function (data, status) {
            alert("Phone fetch failed: " + status);
        });

    $scope.remove = function (phone) {
        $http.delete('/api/phone/' + phone.number)
            .success(function (data, status, headers) {
                var i = $scope.phones.indexOf(phone);
                if (i >= 0) {
                    $scope.phones.splice(i, 1);
                }
            })
            .error(function (data, status) {
                alert("Phone delete failed: " + status);
            });
    };
    $scope.add = function (number) {
        if (!number) {
            console.log("asdasd");
            $scope.showError = true;
            return;
        } else {
            $scope.showError = false;
        }

        $http.put('/api/phone/' + number, {}, {
            headers: {
                'Content-Type': 'application/json'
            }
        })
            .success(function (data, status, headers) {
                console.log(data);
                $scope.phones.push(data);
            })
            .error(function (data, status) {
                if (status != '409') {
                    alert("Phone fetch failed: " + status);
                }
            });
        $scope.addPhone = "";
    }
}]);
