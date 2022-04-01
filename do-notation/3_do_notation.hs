data Project = Project

type Net = Int

type Tax = Int

type Gross = Int

calculateNet :: Project -> Maybe Net
calculateNet = undefined

calculateTax :: Project -> Net -> Maybe Tax
calculateTax = undefined

calculateGross :: Net -> Tax -> Maybe Gross
calculateGross = undefined

calculatePriceDoNotation :: Project -> Maybe (Net, Tax, Gross)
calculatePriceDoNotation project = do
  net <- calculateNet project
  tax <- calculateTax project net
  gross <- calculateGross net tax

  return (net, tax, gross)

-- looks something like that in C#
-- NetTaxGross netTaxGross =
--     from net in calculateNet(project)
--     from tax in calculateTax(project, net)
--     from gross in calculateGross(net, tax)
--     select new NetTaxGross(net, tax, gross);

-- looks something like that in F#
-- netTaxGross = optional {
--   let! net = calculateNet(project)
--   let! tax = calculateTax(project, net)
--   let! gross = calculateGross(net, tax)
--   some(net, tax, gross)
-- }