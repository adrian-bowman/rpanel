/*

This is a somewhat hacky bodge to customise the R Shiny slider
and make it possible to change its rate-limiting behaviour.

We will:

1. Extract the existing slider input binding
//2. Create a new input binding based on the old one, customising getRatePolicy
//3. Register the new binding with a higher priority
2. Bodge it so it returns a different rate policy

*/

// TODO: do I need to require Shiny?

// Get the existing binding for sliders
sliderBinding = Shiny.inputBindings.getBindings().find( b => b.binding.name=="shiny.sliderInput");
//sliderBinding.binding.getRatePolicy = (i => ({policy:"throttle",delay:250}))
sliderBinding.binding.getRatePolicy = (function(i){
  policy = i.attributes["data-rate-policy"] || "debounce";
  delay = i.attributes["data-rate-policy-delay"] || 250;
  console.log(`Returning rate policy for slider of ${policy}, ${delay}`);
  return ({policy:policy,delay:delay});
});
