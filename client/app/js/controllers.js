'use strict';

function AllCtrl ($scope) {
    $scope.test = 'all snippets';
}

function OneCtrl ($scope, $routeParams) {
    $scope.test = 'single snippet';
    $scope.id = $routeParams.id;
}

function VersionCtrl ($scope, $routeParams) {
    $scope.test = 'version';
    $scope.id = $routeParams.id;
    $scope.version = $routeParams.version;
}
