object TilerWebModule: TTilerWebModule
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModuleDefaultHandlerAction
    end
    item
      Name = 'RequestTile'
      PathInfo = '/tiles'
      OnAction = WebModuleTilerRequestTileAction
    end
    item
      Name = 'RequestPointValue'
      PathInfo = '/point'
      OnAction = WebModuleTilerRequestPointValueAction
    end
    item
      Name = 'RequestStatus'
      PathInfo = '/status'
      OnAction = TilerWebModuleRequestStatusAction
    end>
  OnException = WebModuleException
  Height = 179
  Width = 175
end
