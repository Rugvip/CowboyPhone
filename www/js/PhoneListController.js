app.controller('PhoneListCtrl', ['$scope', function($scope) {
    $scope.phones = [
        {number:"16123", callNumber: "1512"},
        {number:"27625", callNumber: "2269"},
        {number:"38641", callNumber: "3302"},/*
        {number:"49642", callNumber: "4282"},
        {number:"51643", callNumber: "5290"},
        {number:"62645", callNumber: "6292"},
        {number:"73629", callNumber: "7285"},*/
        {number:"84646", callNumber: "8286"},
        {number:"95633", callNumber: "9293"}
    ];
    $scope.remove = function (phone) {
        $scope.phones.splice($scope.phones.indexOf(phone), 1);
    };
    $scope.add = function (number) {
        if (!number) {
            number = (Math.random()+"").substr(5, 5);
        }
        $scope.phones.push({number: number, callNumber: ""});
    }
}]);
