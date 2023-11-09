@EndUserText.label: 'Consumption View: Fuel Station'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
define root view entity ZC_RAP_FuelStation
  as projection on ZI_RAP_FuelStation
{
  key StationUuid,
      StationId,
      @Search.defaultSearchElement: true
      StationName,
      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_RAP_StationBrand', element: 'BrandUuid' } }]
      BrandUuid,
      _Brand.BrandName as BrandName,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZI_RAP_StationType', element: 'StationType' } }]
      StationType,
      _StationType.StationTypeName as StationTypeName,
      @Search.defaultSearchElement: true
      Municipality,
      @Search.defaultSearchElement: true
      District,
      Address,
      City,
      PostCode,
      Geolon,
      Geolat,
      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,
      LastChangedAt,

      /* Associations */
      _Brand,
      _Prices : redirected to composition child ZC_RAP_FuelPrice,
      _StationType
}
