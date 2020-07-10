"use strict";

var AWS = require("aws-sdk");

exports.mkDocumentClient = function (region) {
  return new AWS.DynamoDB.DocumentClient({ region });
};

exports.putImpl = function (documentClient, params) {
  documentClient.put(params);
};
