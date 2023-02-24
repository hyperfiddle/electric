goog.provide('hyperfiddle.electric.FailureInfo');

/**
 * @constructor
 */
hyperfiddle.electric.FailureInfo = function(message, data, id, cause) {
  this.message = message;
  this.data = data;
  this.id = id;
  this.cause = cause;
};
