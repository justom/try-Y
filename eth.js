// TODO don't let user use Mainnet

import babel_polyfill from "babel-polyfill"; // TODO move into configuration file (it's only here because I've chosen to use ES2015+)
import Web3 from "web3";
import abi from "./Y_sol_Y.abi"; // FIXME manual build step: having to rename ABI file extension to .abi.json by hand

const web3 = new Web3(Web3.givenProvider || "http://localhost:8545"); // from web3 v1 docs

const address = "0xF4C3aC68Af170E71D2CDFcd3e04964053827A2f8"; // contract address on Rinkeby, deployed from 0xa751fDbcBE2c6Cdcb9aCa517789C3974f930587c

// User chooses which account will be the payee's account by it being the one that calls setNumAndDenom first.

app.ports.setPercent.subscribe(async percent => {
  const contract = new web3.eth.Contract(abi, address);
  const accounts = await web3.eth.getAccounts();
  const payee = accounts[0];
  if ((await web3.eth.net.getNetworkType()) !== "main") {
    try {
      await contract.methods.setNumAndDenom(percent, 100).send({ from: payee }); // FIXME not 100, percent -> rational // FIXME this is trusting that percent will be an integer (it's a float coming from Elm) and valid (Elm's doing this at the moment). // TODO setNum, setDenom or setNumAndDenom
      app.ports.payeeAddress.send(payee);
    } catch (e) {
      console.error(e);
    }
  } else {
    console.error("You're on the main network.");
  }
});

app.ports.getBalances.subscribe(async ({ payee, donees }) => {
  const balances = (await Promise.all(
    [web3.eth.getBalance(payee)].concat(
      donees.map(donee => web3.eth.getBalance(donee))
    )
  )).map(balance => web3.utils.fromWei(balance, "ether"));
  app.ports.balances.send({
    payee: balances[0],
    doneeBalances: balances.slice(1)
  });
});

// app.ports.setPercentAgain.subscribe(async ({ percent, payee }) => {
//   // check for mainnet
//   try {
//     await contract.methods.setNumAndDenom(percent, 100).send({ from: payee }); // FIXME not 100, percent -> rational // FIXME this is trusting that percent will be an integer (it's a float coming from Elm) and valid (Elm's doing this at the moment). // TODO setNum, setDenom or setNumAndDenom
//     app.ports.payeeAddress.send(payee);
//   } catch (e) {
//     console.error(e);
//   }
// });

app.ports.pay.subscribe(async ({ payment, payee, donee }) => {
  const contract = new web3.eth.Contract(abi, address);
  const accounts = await web3.eth.getAccounts();
  const payer = accounts[0]; // FIXME assumes user has switched accounts in MetaMask before paying. Only let user pay if active account in MetaMask isn't the payee's
  if (payer !== payee) {
    // stops user paying from payee's account
    if ((await web3.eth.net.getNetworkType()) !== "main") {
      contract.methods
        .payAndDonate(payee, donee)
        .send({ from: payer, value: web3.utils.toWei(payment, "ether") })
        .on("transactionHash", hash => app.ports.paid.send(hash))
        .on("error", console.error);
      // FIXME the donee's balances haven't always gone up by the time the paid port is called, so the balances on the screen don't go up after a payment.
    } else {
      console.error("You're on the main network.");
    }
  } else {
    console.error("The payer and payee are the same");
  }
});
