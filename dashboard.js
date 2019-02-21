import { PageBuilder } from "admin-bro"
import { User, Environment } from "./postgresql/models/index"
import { Op } from "sequelize"

class DashboardPage extends PageBuilder {
  constructor(props) {
    super(props)
    this.title = "Igloo Bering"
  }

  async build() {
    const userCount = await User.count({
      where: { [Op.not]: { email: { [Op.like]: "%@igloo.ooo" } } },
    })
    this.addBlock({
      title: "Users",
      value: userCount,
      icon: "fas fa-users fa-2x",
      columns: 3,
    })
    const userCountBusiness = await User.count({
      where: {
        paymentPlan: "BUSINESS",
        [Op.not]: { email: { [Op.like]: "%@igloo.ooo" } },
      },
    })
    this.addBlock({
      title: "Business users",
      value: userCountBusiness,
      icon: "fas fa-dollar-sign fa-2x",
      columns: 3,
    })

    const environmentCount = await Environment.count({
      where: {},
    })
    this.addBlock({
      title: "Environments",
      value: environmentCount,
      icon: "fas fa-columns fa-2x",
      columns: 3,
    })
  }
}

module.exports = DashboardPage
