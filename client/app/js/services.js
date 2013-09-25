'use strict';

window.angular
    .module('snippet.services', ['ngResource'])

    .factory('Snippet', function ($resource) {
        return $resource(
            'http://54.200.60.8\\:8000/snippet/:id',
            { },
            {
                query: {
                    isArray: true,
                    method:'GET',
                    params: { id: '' }
                }
            }
        );
    })

    .factory('Comment', function ($resource) {
        return $resource(
            'http://54.200.60.8\\:8000/comment/:id',
            { },
            {
                query: {
                    isArray: true,
                    method:'GET',
                    params: { id: '' }
                }
            }
        );
    });
