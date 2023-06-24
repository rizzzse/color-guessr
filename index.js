import "./src/index.scss";

if (import.meta.env.DEV) {
  const { main } = await import("./output/Main/index.js");
  main();
} else {
  import("./dist/index.js");
}
