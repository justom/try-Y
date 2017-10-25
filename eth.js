// FIXME don't let user use Mainnet: at the moment, each function that could interact with mainnet, including spending Ether, is surrounded by an if statement to guard against use of mainnet. Having to manually surrounding each function like this is error-prone. Ideally, the application that calls the functions in this file via app.ports should sense what network it's on and not call those functions if it's on mainnet.

import babel_polyfill from "babel-polyfill"; // TODO move into configuration file (it's only here because I've chosen to use ES2015+)
import Web3 from "web3";
import abi from "./Y_sol_Y.abi"; // FIXME manual build step: having to rename ABI file extension to .abi.json by hand
import bigRat from "big-rational";

const web3 = new Web3(Web3.givenProvider); // from web3 v1 docs

const address = "0xF4C3aC68Af170E71D2CDFcd3e04964053827A2f8"; // contract address on Rinkeby

app.ports.haveMetaMask.subscribe(
  () => app.ports.hasMetaMask.send(true) // FIXME detect MetaMask
);

app.ports.getAccount.subscribe(async () =>
  app.ports.accountGot.send((await web3.eth.getAccounts())[0])
);

app.ports.setPercent.subscribe(async percent => {
  const contract = new web3.eth.Contract(abi, address);
  const accounts = await web3.eth.getAccounts();
  const payee = accounts[0];
  const rational = bigRat(percent).divide(100); // 7 -> 7%
  if ((await web3.eth.net.getNetworkType()) !== "main") {
    contract.methods
      .setNumAndDenom(rational.num.valueOf(), rational.denom.valueOf()) // NOTE this is trusting that percent will be valid (Elm's doing this at the moment). // TODO setNum, setDenom or setNumAndDenom
      .send({ from: payee }) // User chooses which account will be the payee's account by it being the one that calls setNumAndDenom first.
      .on("transactionHash", _ => {
        app.ports.settingPercent.send(null);
        app.ports.payeeAddress.send(payee);
      }) // FIXME test-ether: Why is txHash being used to determine payee's address? Payee becomes payee when they set a % on the contract, therefore on receipt.
      .on("receipt", _ => app.ports.percentSet.send(null))
      .error(console.error);
  } else {
    console.error("You're on the main network."); // FIXME Put this error on the page: send it to Elm (percentSet : Maybe (https://guide.elm-lang.org/interop/javascript.html)).
  }
});

app.ports.payerGetPercent.subscribe(async payee => {
  const contract = new web3.eth.Contract(abi, address);
  const numAndDenom = await Promise.all([
    contract.methods.nums(payee).call(),
    contract.methods.denoms(payee).call() // NOTE two calls here. Does using a struct for num and denom (one call) cost more gas to set? UPDATE: It's about whether payAndDonate is cheaper or not, as that's the most frequent.
  ]);
  const percent = bigRat(numAndDenom[0], numAndDenom[1])
    .multiply(100)
    .toDecimal(); // 7% -> 7
  app.ports.percentGot.send(percent);
});

app.ports.getBalances.subscribe(async ({ payee, donees }) => {
  if ((await web3.eth.net.getNetworkType()) !== "main") {
    const balances = (await Promise.all(
      [web3.eth.getBalance(payee)].concat(
        donees.map(donee => web3.eth.getBalance(donee))
      )
    )).map(balance => web3.utils.fromWei(balance, "ether"));
    app.ports.balances.send({
      payee: balances[0],
      doneeBalances: balances.slice(1)
    });
  } else {
    console.error("You're on the main network.");
  }
});

app.ports.pay.subscribe(async ({ payment, payee, donee }) => {
  const contract = new web3.eth.Contract(abi, address);
  const accounts = await web3.eth.getAccounts();
  const payer = accounts[0];
  if (payer !== payee) {
    // stops user paying from payee's account
    if ((await web3.eth.net.getNetworkType()) !== "main") {
      contract.methods
        .payAndDonate(payee, donee)
        .send({ from: payer, value: web3.utils.toWei(payment, "ether") })
        .on("transactionHash", _ => app.ports.paying.send(null))
        // .on("receipt", _ => app.ports.paid.send(null))
        // When you get a receipt it says that you sent x wei to address y (the contract) with the payee's address in tow (the input data). You don't get a receipt until the transaction has been accepted. So what's the point of confirmations after that? What can go wrong after a receipt? A receipt is a "proof of payment", right?
        .on("confirmation", _ => app.ports.paid.send(null))
        .on("error", console.error);
    } else {
      console.error("You're on the main network.");
    }
  } else {
    console.error("The payer and payee are the same.");
  }
});
