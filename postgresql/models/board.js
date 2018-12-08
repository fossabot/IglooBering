module.exports = (queryInterface, Sequelize) => {
  const Board = queryInterface.define("board", {
    id: {
      type: Sequelize.UUID,
      defaultValue: Sequelize.UUIDV4,
      primaryKey: true,
    },
    name: {
      type: Sequelize.STRING,
      allowNull: false,
    },
    avatar: {
      type: Sequelize.STRING,
    },
    index: {
      type: Sequelize.INTEGER,
    },
    muted: {
      type: Sequelize.BOOLEAN,
    },
  })
  Board.associate = function(models) {
    // associations can be defined here
  }
  return Board
}
