"use strict";

var AWS = require("aws-sdk");

exports.mkDynamoDB = function (region) {
  return new AWS.DynamoDB({ region });
};
