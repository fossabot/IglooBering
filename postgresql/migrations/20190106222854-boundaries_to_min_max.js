"use strict"

module.exports = {
  up: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("floatValues", "min", {
      type: Sequelize.FLOAT,
    })
    await queryInterface.addColumn("floatValues", "max", {
      type: Sequelize.FLOAT,
    })
    await queryInterface.sequelize.query(
      `UPDATE public."floatValues" SET min = boundaries[1]`
    )
    await queryInterface.sequelize.query(
      `UPDATE public."floatValues" SET max = boundaries[2]`
    )
    await queryInterface.removeColumn("floatValues", "boundaries")

    await queryInterface.addColumn("plotValues", "min", {
      type: Sequelize.FLOAT,
    })
    await queryInterface.addColumn("plotValues", "max", {
      type: Sequelize.FLOAT,
    })

    await queryInterface.sequelize.query(
      `UPDATE public."plotValues" SET min = boundaries[1]`
    )
    await queryInterface.sequelize.query(
      `UPDATE public."plotValues" SET max = boundaries[2]`
    )

    await queryInterface.removeColumn("plotValues", "boundaries")
  },

  down: async (queryInterface, Sequelize) => {
    await queryInterface.addColumn("floatValues", "boundaries", {
      type: Sequelize.ARRAY(Sequelize.FLOAT),
    })
    await queryInterface.sequelize.query(
      `UPDATE public."floatValues" SET boundaries [1] = min, boundaries [2] = max WHERE min IS NOT NULL AND max IS NOT NULL`
    )
    await queryInterface.removeColumn("floatValues", "min")
    await queryInterface.removeColumn("floatValues", "max")

    await queryInterface.addColumn("plotValues", "boundaries", {
      type: Sequelize.ARRAY(Sequelize.FLOAT),
    })
    await queryInterface.sequelize.query(
      `UPDATE public."plotValues" SET boundaries [1] = min, boundaries [2] = max WHERE min IS NOT NULL AND max IS NOT NULL`
    )
    await queryInterface.removeColumn("plotValues", "min")
    await queryInterface.removeColumn("plotValues", "max")
  },
}
