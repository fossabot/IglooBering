"use strict"

const replaceEnum = require("sequelize-replace-enum-postgres").default

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await replaceEnum({
      queryInterface,
      tableName: "floatValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE"],
      enumName: "enum_floatValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "stringValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE"],
      enumName: "enum_stringValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "booleanValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE"],
      enumName: "enum_booleanValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "plotValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE"],
      enumName: "enum_plotValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "categoryPlotValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE"],
      enumName: "enum_stringPlotValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "mapValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE"],
      enumName: "enum_mapValues_tileSize",
    })
  },

  down: async (queryInterface, Sequelize) => {
    await replaceEnum({
      queryInterface,
      tableName: "floatValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE", "TALL"],
      enumName: "enum_floatValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "stringValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE", "TALL"],
      enumName: "enum_stringValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "booleanValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE", "TALL"],
      enumName: "enum_booleanValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "plotValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE", "TALL"],
      enumName: "enum_plotValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "categoryPlotValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE", "TALL"],
      enumName: "enum_stringPlotValues_tileSize",
    })
    await replaceEnum({
      queryInterface,
      tableName: "mapValues",
      columnName: "cardSize",
      defaultValue: "NORMAL",
      newValues: ["NORMAL", "WIDE", "LARGE", "TALL"],
      enumName: "enum_mapValues_tileSize",
    })
  },
}
