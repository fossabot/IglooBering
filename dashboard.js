import { PageBuilder } from "admin-bro"
import { User, Environment } from "./postgresql/models/index"
import { Op } from "sequelize"

class DashboardPage extends PageBuilder {
  constructor(props) {
    super(props)
    this.title = "Igloo Bering"
  }

  async build() {
    const userCountFree = await User.count({
      where: {
        paymentPlan: "FREE",
        // [Op.not]: { email: { [Op.like]: "%@igloo.ooo" } },
      },
    })
    const userCountBusiness = await User.count({
      where: {
        paymentPlan: "BUSINESS",
        // [Op.not]: { email: { [Op.like]: "%@igloo.ooo" } },
      },
    })
    this.addChart({
      columns: 6,
      title: "Total Users",
      config: {
        type: "bar",
        data: {
          datasets: [
            {
              label: "FREE",
              fill: true,
              backgroundColor: PageBuilder.COLOR.INFO,
              data: [userCountFree],
            },
            {
              label: "BUSINESS",
              fill: true,
              backgroundColor: PageBuilder.COLOR.WARNING,
              data: [userCountBusiness],
            },
          ],
        },
      },
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
