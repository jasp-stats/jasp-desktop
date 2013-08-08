
if (_.has(results, "descriptives"))
    item.tables( { tables : [ results.ttest, results.descriptives ] } )
else
    item.tables( { tables : [ results.ttest ] } )
