object WebModule1: TWebModule1
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModule1DefaultHandlerAction
    end
    item
      Name = 'GPSToBuildingHandler'
      PathInfo = '/gpstobuilding'
      OnAction = WebModule1GPSToBuildingHandlerAction
    end
    item
      Name = 'AllBuildingsHandler'
      PathInfo = '/allbuildings'
      OnAction = WebModule1AllBuildingsHandlerAction
    end>
  OnException = WebModuleException
  Height = 230
  Width = 415
end
