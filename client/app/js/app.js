'use strict';

// Declare app level module which depends on filters and services.
window.angular
    .module('snippets', [])

    .config(['$routeProvider', function($routeProvider) {
        $routeProvider.when('/snippet', {
            controller: 'AllCtrl',
            templateUrl: 'partials/all.html'
        });

        $routeProvider.when('/snippet/:id', {
            controller: 'OneCtrl',
            templateUrl: 'partials/one.html'
        });

        $routeProvider.when('/snippet/:id/:version', {
            controller: 'VersionCtrl',
            templateUrl: 'partials/version.html'
        });

        $routeProvider.otherwise({
            redirectTo: '/snippet'
        });
    }]);
