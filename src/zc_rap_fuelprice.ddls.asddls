@EndUserText.label: 'Consumption View: Fuel Price'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define view entity ZC_RAP_FuelPrice
  as projection on ZI_RAP_FuelPrice
{
  key StationUuid,
  key PriceUuid,
      PriceID,
      //@Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_RAP_FuelType', element: 'FuelTypeId' } }]
      FuelType,
      UpdateDate,
      Price,
      _FuelType.FuelTypeName as FuelTypeName,
      /* Associations */
      _FuelType,
      _Station : redirected to parent ZC_RAP_FuelStation
}
