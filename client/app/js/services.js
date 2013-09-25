'use strict';

window.angular
    .module('snippet.services', ['ngResource'])

    .factory('Snippet', function ($resource) {
        return $resource(
            'json/:id.json',
            { },
            {
                query: {
                    method:'GET',
                    params: { id: 'snippets' },
                    isArray: true
                }
            }
        );
    });
