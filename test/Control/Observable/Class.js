'use strict';

var Observable = require('zen-observable');

var _from = function(instanceDict) {
  return function(iterableOrObservable) {
    return function() {
      return Observable.from(iterableOrObservable);
    };
  };
};

exports._fromIterable = _from;
exports._fromObservable = _from();

exports._of = function(a) {
  return function() {
    return Observable.of(a);
  };
};

exports._subscribe = function(observable) {
  return function(listeners) {
    return function() {
      return observable.subscribe(listeners);
    };
  };
};

exports._map = function(fn) {
  return function(observable) {
    return observable.map(fn);
  };
};
