const { handler, unsafeHandler } = require("../output/AffEvent");

//handler({ id: 1, text: "Yo Dawg" }).then((r) => console.log(r));
//handler({ id: 1, message: "Yo Dawg" }).then((r) => console.log(r));
unsafeHandler({ id: 1, text: "Yo Dawg" }).then((r) => console.log(r));
