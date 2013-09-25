'use strict';

window.angular
    .module('snippet.controllers', ['snippet.services'])

    .controller('AllCtrl', ['$scope', 'Snippet', function ($scope, Snippet) {
        $scope.snippets = Snippet.query();
    }])

    .controller('OneCtrl', ['$scope', '$routeParams', 'Snippet', function ($scope, $routeParams, Snippet) {
        $scope.snippet = Snippet.get({
            id: $routeParams.id
        });
    }])

    .controller('VersionCtrl', ['$scope', '$routeParams', 'Snippet', function ($scope, $routeParams, Snippet) {
        $scope.snippet = Snippet.get({
            id: $routeParams.id,
            version: $routeParams.version
        });
    }]);
