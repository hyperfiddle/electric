goog.provide('hyperfiddle.electric.FailureInfo');

/**
 * @constructor
 */
hyperfiddle.electric.FailureInfo = function(message, data, cause) {
  this.message = message;
  this.data = data;
  this.cause = cause;
};
