'use strict';

import ModuleToastsController from 'components/moduleControl/moduleToastsController';
import ModelCollection from 'core/modelCollection';
import ModuleModel, { ModuleStatus } from 'components/moduleControl/moduleModel';

describe('ModuleToastsController', () => {
    let modules;
    let toastFunction;
    let toastsController;
    let moduleModel;
    
    beforeEach(() => {
        modules = new ModelCollection();
        toastFunction = jest.fn();
        toastsController = new ModuleToastsController({
            modules,
            toastFunction
        });

        moduleModel = new ModuleModel({
            id: 'Air1',
            name: 'Air Quality',
            progress: 12345,
            status: ModuleStatus.IDLE
        });

        modules.add(moduleModel);
    });

    it('no toast are shown for uninteresting module statuses', ()=> {
        moduleModel.status = ModuleStatus.IDLE;
        moduleModel.status = ModuleStatus.LOCKED;
        moduleModel.status = ModuleStatus.REMOVED;

        expect(toastFunction).not.toHaveBeenCalled();
    });
    
    it('show toast when module becomes busy', () => {
        moduleModel.status = ModuleStatus.BUSY;

        expect(toastFunction).toHaveBeenCalledTimes(1);
        expect(toastFunction).toHaveBeenCalledWith('Air Quality module is BUSY', 'succes', 5000);
    });

    it('show toast when module becomes calculating', () => {
        moduleModel.status = ModuleStatus.CALCULATING;

        expect(toastFunction).toHaveBeenCalledTimes(1);
        expect(toastFunction).toHaveBeenCalledWith('Air Quality module is BUSY', 'succes', 5000);
    });

    it('show toast when module becomes ready', () => {
        moduleModel.status = ModuleStatus.READY;

        expect(toastFunction).toHaveBeenCalledTimes(1);
        expect(toastFunction).toHaveBeenCalledWith('Air Quality module is READY', 'succes', 5000);
    });

});
